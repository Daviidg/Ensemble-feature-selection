find ../datasets -type f -exec du -kh {} + | sort -h

find ../datasets -type f -exec du -kh {} + | sort -h | awk '{print $2}' | \
    xargs -I {} Rscript ensemble.R {}
