{
  pkgs ? import <nixpkgs> { },
}:

{
  env = pkgs.mkShell {
    packages = with pkgs; [
      coreutils
      gcc
      # emacs
      curl
      sbcl
      sbclPackages.ironclad
      sbclPackages.bordeaux-threads
      sbclPackages.lparallel
      sbclPackages.cl-dbi
      sbclPackages.clog
      asdf
      # openssl.out

      #git
    ];
    # This shell hook is a must for libs that require dynamic libraries to be found
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath ([ pkgs.openssl ])}:${
        pkgs.lib.makeLibraryPath ([ pkgs.sqlite ])
      }
    '';
    # pathsToLink = [
    #   "/share"
    #   "/bin"
    #   "/lib"
    #   "/include"
    # ];
    # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.openssl_1_1 ];
    # extraOutputsToInstall = [ "lib" ];
  };
}
