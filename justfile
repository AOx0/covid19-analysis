default: publish

push:
    cd $PAGE && git commit -am "Updated covid"
    cd $PAGE && git push 

publish: && push
    rm -rf $PAGE/covid
    cp -rf out $PAGE/covid