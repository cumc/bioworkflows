To build and upload the docker container for the GATK+ANNOVAR pipeline,

```
docker build --build-arg DUMMY=`date +%s` -t gaow/gatk4-annovar -f gatk4-annovar.dockerfile . 
docker push gaow/gatk4-annovar
```

to build singularity container,

```
spython recipe gatk4-annovar.dockerfile | sed 's/Stage: spython-base//g' &> gatk4-annovar.def
singularity build --fakeroot gatk4-annovar.sif gatk4-annovar.def
```
