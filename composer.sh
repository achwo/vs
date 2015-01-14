#!/bin/sh
compile () {
  [ $# -lt 2 ] && {
    echo "wrong number of arguments"
    return 1
  }

  local dst="$1"
  shift
  #local src="$2"

  while [ $# -ge 1 ]; do
    local src="$1"
    shift

    echo "compiling $src"
    erlc -o "$dst" "$src"
  done

  return 0
}

compile_test () {
  [ $# -lt 2 ] && {
    echo "wrong number of arguments"
    return 1
  }

  local dst="$1"
  shift
  #local src="$2"

  while [ $# -ge 1 ]; do
    local src="$1"
    shift

    echo "compiling $src"
    erlc +export_all -o "$dst" "$src"
  done

  return 0
}

run_test () {
  erl -pa bin -s "$1" test -noshell -run init stop
}

run () {
  local name="$1"
  local module="$2"
  cd bin && erl \
    -name $name \
    -setcookie blutwurst \
    -run $module start \
    -run init stop -noshell
}

#
main () {
  # setup build variables
  local ECC=erlc
  local ERL=erl
  local DST=bin

  local cmd="$1"
  shift

  case "$cmd" in
    "build") {
      compile $DST test/*.erl
      compile $DST src/*.erl
    };;

    "test") {
      if [ "$1" = "" ]; then
        echo "nooooooo"
        exit 42
      fi
      compile_test $DST test/*.erl
      compile_test $DST src/*.erl
      run_test "$1"
    };;

    *) {
      echo "unknwon command $cmd"
      exit 1
    };;
  esac
}

main $@
