BEGIN {
	FS = ",";
	OFS = ",";
}

NR > 1 && $2 != "" {
	if (NR==FNR) {
		parent[$1 $2] = $0
		next;
	}
	{
		key = $1 $2
		if (key in parent) {
			print parent[key], $3
		} else {
			print "NO MATCH ON", key
		}
	}

}
