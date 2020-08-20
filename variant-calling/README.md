To build and upload the docker container for the GATK+ANNOVAR pipeline,

```
docker build --build-arg DUMMY=`date +%s` -t gaow/gatk4-annovar -f gatk4-annovar.dockerfile . 
docker push gaow/gatk4-annovar
```
