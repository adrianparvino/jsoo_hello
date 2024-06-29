with import <nixpkgs> {};
pkgs.mkShell {
  buildInputs = [ opam wrangler nodejs nodePackages.uglify-js esbuild ];

  shellHook = ''
    eval $(opam env)
  '';
}
