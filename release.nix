{ haskellPackages ? (import dep/obelisk {}).obelisk }:
let
  haskellPackages' = haskellPackages.extend (self: super: {
    obelisk-google-analytics = self.callCabal2nix "obelisk-google-analytics" ./. {};
  });
in
{
  inherit haskellPackages';
  inherit (haskellPackages') obelisk-google-analytics;
}
