-- Interpret using this command:
--    podman run -it --rm -v $(pwd):/data:Z quay.io/software-factory/shake-factory
--
-- Learn more at: https://softwarefactory-project.io/cgit/software-factory/shake-factory/tree/README.md

import Development.Shake
import ShakeFactory
import ShakeFactory.Dhall

main = shakeMain $ do
  want ["conf/schemas/package.dhall"]
  "conf/schemas.dhall" %> dhallTopLevelPackageAction "./conf/schemas/package.dhall"
  "//package.dhall" %> dhallPackageAction
  cleanRules
