#!/bin/sh

## Write the shell script
cat > .run-pre.sh <<EOF
#!/bin/bash	
echo "**** Job starts ****"
date

# run pre
R --min-vsize=3G --min-nsize=10M -e "source('pre-small.R')"

echo "**** Job ends ****"
date
EOF

## Do the call
call="qsub -cwd -l mem_free=15G,h_vmem=30G,h_fsize=10G -N run-pre -m e .run-pre.sh"
echo $call
$call