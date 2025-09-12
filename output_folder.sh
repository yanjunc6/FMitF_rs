for file in $(find $1 -type f -name "*.rs"); do
  echo "\`\`\` $file"
  cat "$file"
  echo "\`\`\`"
done