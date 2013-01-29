package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

public class FileStreamFactory implements StreamFactory {

    @Override
    public InputStream getStreamFromFile(String fileName) {

        File file = new File(fileName);

        if(!file.exists()){
            throw new DocGenException("Failed to load fileName for resource: " + fileName);
        }

        InputStream stream;

        try {
            stream = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            throw new DocGenException("Failed to load fileName for resource: " + fileName);
        }

        return stream;
    }
}
