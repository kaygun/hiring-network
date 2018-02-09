SRC=src
DATA=data
FIGURES=figures

case $1 in
  countries|universities)
     rm $DATA/$1-*.csv
     rm $FIGURES/$1-*.{dot,png,svg}
  ;;
esac

