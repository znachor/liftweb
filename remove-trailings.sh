find . -type f -name '*.scala' -exec sed -i 's/[ \t]*$//' {} \;

