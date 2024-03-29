with import <nixpkgs> {};
pkgs.mkShell {
  buildInputs = [ opam wrangler nodejs nodePackages.terser closurecompiler ];

  shellHook = ''
    eval $(opam env)
  '';
}
