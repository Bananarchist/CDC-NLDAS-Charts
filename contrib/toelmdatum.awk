BEGIN {
	FS=",";
}

{ 
	printf "Datum \"%s\" %s %f %f\n,  ",$1,$2,$3,$4;
}
