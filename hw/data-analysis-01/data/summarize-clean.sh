#!/bin/sh

## Write the shell script
cat > .run-sum-clean.sh <<EOF
#!/bin/bash	
echo "**** Job starts ****"
date

# run pre
R --min-vsize=3G --min-nsize=10M -e "source('summarize-clean.R')"

echo "**** Job ends ****"
date
EOF

## Do the call
call="qsub -cwd -l mem_free=15G,h_vmem=30G,h_fsize=10G -N sum-clean -m e .run-sum-clean.sh"
echo $call
$call