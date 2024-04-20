{
  description = "blog.dziban.net";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs: 
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { config, self', pkgs, ... }: {
        packages = {
	  dziban-blog = pkgs.stdenv.mkDerivation {
	    name = "dziban-blog";
	    depsBuildBuild = [ pkgs.zola ];
	    src = ./.;

	    buildPhase = ''
	    zola build
	    '';

	    installPhase = ''
	    mkdir -p $out
	    mv public/* $out/
	    '';
	  };

	  default = self'.packages.dziban-blog;
	};
      };
    };
}
