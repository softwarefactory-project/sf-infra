#!/usr/bin/python
# Copyright © 2025 Red Hat
# SPDX-License-Identifier: Apache-2.0


# Utility to rotate all ansible-vault-encrypted secrets with
# a newly generated secret.


import argparse
import glob
import logging
import os
import sys
import re
import subprocess
import secrets
import tempfile
import textwrap


logger = logging.getLogger('rotate-ansible-vault-password')
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(sys.stdout)
handler.setLevel(logging.DEBUG)
logger.addHandler(handler)


zuul_secrets_file_path = 'zuul.d/secrets.yaml'


def do_file(file_path, old_vault_file, new_vault_file, dry_run=False):
    new_file_contents = []
    vault_secret_prefix_regexp = r'^(\s*)(\S+): !vault \|\n'
    initial_indent = ""
    vault_secret_regexp =\
        r'[\s\t]+(\$ANSIBLE_VAULT;1.1;AES256|[0-9a-fA-F]+)\n?$'
    secret_contents = []
    secret_started = False
    line_count = 0
    with open(file_path, 'r') as f:
        for orig_line in f.readlines():
            line_count += 1
            # Is it the start of a secret?
            _secret_start = re.match(vault_secret_prefix_regexp, orig_line)
            if _secret_start:
                initial_indent = _secret_start.group(1)
                secret_name = _secret_start.group(2)
                logger.info("secret found: %s" % secret_name)
                secret_started = True
                new_file_contents.append(orig_line)
                continue
            # Is it secret data?
            _secret_line = re.match(vault_secret_regexp, orig_line)
            if secret_started and _secret_line:
                secret_line = _secret_line.group(0)
                secret_contents.append(secret_line)
            else:
                if secret_contents:
                    # We have the whole secret, let's decrypt it ...
                    clean_blob = '\n'.join(ln.strip()
                                           for ln in secret_contents)
                    try:
                        decrypt_proc = subprocess.run(
                            ['ansible-vault', 'decrypt',
                             '--vault-password-file', old_vault_file],
                            input=clean_blob, capture_output=True,
                            text=True, check=True
                        )
                        decrypted = decrypt_proc.stdout
                    except subprocess.CalledProcessError as e:
                        line_no = line_count - len(secret_contents)
                        logger.error(
                            "Error decrypting "
                            f"'{secret_name}' on line "
                            f"{line_no} in {file_path}: {e.stderr}")
                        sys.exit(1)
                    if not dry_run:
                        # Then re-encrypt it ...
                        try:
                            encrypt_proc = subprocess.run(
                                ['ansible-vault', 'encrypt_string',
                                 '--vault-password-file',
                                 new_vault_file, '--stdin-name',
                                 'dummy'],
                                input=decrypted,
                                capture_output=True,
                                text=True,
                                check=True
                            )
                            encrypted_output = encrypt_proc.stdout
                            # ansible-vault calls the secret "dummy" by
                            # default so we must strip the first line
                            encrypted_output = encrypted_output.\
                                split('\n')[1:]

                        except subprocess.CalledProcessError as e:
                            logger.error("Error encrypting in "
                                         f"{file_path}: {e.stderr}")
                            sys.exit(1)
                        for encrypted_line in encrypted_output:
                            stripped = encrypted_line.strip()
                            if len(stripped) > 0:
                                indented = (
                                    initial_indent
                                    + "          %s\n" % stripped)
                                new_file_contents.append(indented)
                    else:
                        new_file_contents += secret_contents
                    secret_started = False
                    initial_indent = ""
                    secret_contents = []
                # in any case, append the current line
                new_file_contents.append(orig_line)
    return ''.join(new_file_contents)


def do_zuul_secret(vault_password,
                   zuul_url="https://gateway-cloud-softwarefactory.apps."
                            "ocp.cloud.ci.centos.org/zuul",
                   zuul_tenant='sf',
                   zuul_project='software-factory/sf-infra'):
    try:
        from zuulclient.api import ZuulRESTClient
        from zuulclient.utils import encrypt_with_openssl
    except ImportError:
        logger.error("Install the zuul-client python "
                     "library before running this script")
        sys.exit(1)
    zc = ZuulRESTClient(zuul_url, verify=True)
    public_key = zc.get_key(zuul_tenant, zuul_project)
    pubkey_file = tempfile.NamedTemporaryFile(delete=False)
    logger.debug('Creating temporary key file %s' % pubkey_file.name)
    pubkey_file.write(str.encode(public_key))
    pubkey_file.close()
    end_in_error = False
    try:
        ciphertext_chunks = encrypt_with_openssl(pubkey_file.name,
                                                 vault_password,
                                                 logger)
        output = ('''    data:
      secret: !encrypted/pkcs1-oaep
''')

        twrap = textwrap.TextWrapper(width=79,
                                     initial_indent=' ' * 8,
                                     subsequent_indent=' ' * 10)
        for chunk in ciphertext_chunks:
            chunk = twrap.fill('- ' + chunk)
            output += chunk + '\n'
        output += '\n'

    except Exception as e:
        logger.error('Error occurred during Zuul secret encryption: %s' % e)
        end_in_error = True
    finally:
        os.unlink(pubkey_file.name)
        if end_in_error:
            sys.exit(1)

    # replace secret in file
    new_zuul_secrets_contents = []
    old_vault_secret = [
        '\n',
        '- secret:\n',
        '    name: old-ansible-vault\n',
    ]
    marker = False
    with open(zuul_secrets_file_path, 'r') as secrets_yaml:
        for ln in secrets_yaml.readlines():
            if marker is True:
                if '- secret:' in ln:
                    marker = False
                    new_zuul_secrets_contents.append(output)
                    new_zuul_secrets_contents.append('\n')
                    new_zuul_secrets_contents.append('- secret:\n')
                else:
                    old_vault_secret.append(ln)
            else:
                if 'name: ansible-vault' in ln:
                    marker = True
                new_zuul_secrets_contents.append(ln)
    # keep the old secret in case of an issue
    new_zuul_secrets_contents += old_vault_secret
    return ''.join(new_zuul_secrets_contents)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="rotate-ansible-vault-password",
        description="utility that can be used to validate existing "
                    "secrets encoded with ansible-vault, "
                    "or rotate the vault's password and re-encrypt "
                    "all secrets on the fly."
    )
    parser.add_argument(
        'vault_password_file',
        help='path to a file containing the current Ansible vault password')
    parser.add_argument(
        '-d',
        '--dry-run',
        action='store_true',
        help='Use this flag to only validate encrypted secrets '
             'against the current ansible vault secret')
    parser.add_argument(
        '-s',
        '--show-secret',
        action='store_true',
        help='Show the new password after the rotation')

    args = parser.parse_args()
    old_pass_file = args.vault_password_file
    dry_run = args.dry_run
    show_secret = args.show_secret
    new_vault_secret = secrets.token_urlsafe(16)
    with tempfile.NamedTemporaryFile(delete_on_close=False) as vault_file:
        vault_file.write(new_vault_secret.encode())
        vault_file.close()
        logger.debug('New vault password stored '
                     'temporarily in %s' % vault_file.name)

        # TODO sf-infra repo path as argument - the script assumes
        # it's being run from the repo's root dir
        playbooks_dir = glob.glob('playbooks/**/*.y*ml', recursive=True)
        roles_dir = glob.glob('roles/**/*.y*ml', recursive=True)

        for f in playbooks_dir:
            logger.info("Parsing %s" % f)
            new_file = do_file(f, old_pass_file, vault_file.name, dry_run)
            if not dry_run:
                with open(f, 'w') as _f:
                    _f.write(''.join(new_file))

        for f in roles_dir:
            logger.info("Parsing %s" % f)
            new_file = do_file(f, old_pass_file, vault_file.name, dry_run)
            if not dry_run:
                with open(f, 'w') as _f:
                    _f.write(''.join(new_file))

    if not dry_run:
        logger.info("Encoding new password into zuul secret...")
        zuul_secrets = do_zuul_secret(new_vault_secret)
        with open(zuul_secrets_file_path, 'w') as zsf:
            zsf.write(zuul_secrets)

        if show_secret:
            logger.info("New vault secret: %s" % new_vault_secret)
        logger.info("Secrets rotation complete.")
    else:
        logger.info("Secrets validated successfully.")
