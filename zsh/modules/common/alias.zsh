function alias_value() {
    (( $+aliases[$1] )) && echo $aliases[$1]
}

function try_alias_value() {
    alias_value "$1" || echo "$1"
}
