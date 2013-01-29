package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.InputStream;

public class ClassPathStreamFactory implements StreamFactory {

    @Override
    public InputStream getStreamFromFile(String fileName) {

        Class<? extends ClassPathStreamFactory> aClass = getClass();

        InputStream stream = aClass.getResourceAsStream(fileName);

        if(stream == null){
            throw new DocGenException("Failed to load stream for resource: " + fileName);
        }

        return stream;
    }

}
