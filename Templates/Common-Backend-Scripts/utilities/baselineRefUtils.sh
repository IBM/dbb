# catBaselineRefFile <file>
#  Outputs the contents of baselineRef.yaml to stdout in IBM-1047 encoding,
#  regardless of how the file is tagged on USS. On z/OS, YAML files managed
#  by Git are typically tagged UTF-8. Reading them directly with grep in an
#  IBM-1047 shell session yields garbled output because the encodings differ.
#  This function detects the file tag via 'chtag -p' and pipes through iconv
#  only when necessary. The file on disk is never modified.
#
#  When iconv is needed, the converted output is written to a tagged temporary
#  file so that the USS kernel does not re-convert the bytes a second time on
#  the subsequent cat/read. The temporary file is removed after use.

catBaselineRefFile() {
    local file="$1"
    # chtag -p prints one of: "t IBM-1047 <file>", "t UTF-8 <file>", "b <file>" (binary/untagged)
    local fileTag
    fileTag=$(chtag -p "${file}" 2>/dev/null | awk '{ print $2 }' | tr '[:lower:]' '[:upper:]')
    case "${fileTag}" in
        UTF-8 | UTF8 | ISO8859-1 | ISO88591)
            # Convert to IBM-1047 and write to a temporary file.
            # Tag the temp file as IBM-1047 so the USS kernel does not attempt
            # a second auto-conversion when the file is read back.
            # mktemp is not available on z/OS USS; use $$ (PID) for uniqueness.
            local tmpFile
            tmpFile="/tmp/baselineRef_$$.tmp"
            iconv -f "${fileTag}" -t IBM-1047 "${file}" > "${tmpFile}"
            chtag -t -c IBM-1047 "${tmpFile}"
            cat "${tmpFile}"
            rm -f "${tmpFile}"
            ;;
        *)
            # IBM-1047, untagged, or binary - cat as-is
            cat "${file}"
            ;;
    esac
}
