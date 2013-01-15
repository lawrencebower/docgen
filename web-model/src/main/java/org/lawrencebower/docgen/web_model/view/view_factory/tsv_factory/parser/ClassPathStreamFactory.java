package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.InputStream;
import java.io.InputStreamReader;

public class ClassPathStreamFactory implements StreamFactory {

    @Override
    public InputStreamReader getStreamFromFile(String fileName) {

        Class<? extends ClassPathStreamFactory> aClass = getClass();

        InputStream stream = aClass.getResourceAsStream(fileName);

        if(stream == null){
            throw new DocGenException("Failed to load stream for resource: " + fileName);
        }

        return new InputStreamReader(stream);
    }

}
