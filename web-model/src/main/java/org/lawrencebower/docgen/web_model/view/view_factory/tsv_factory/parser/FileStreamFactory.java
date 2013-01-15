package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;

public class FileStreamFactory implements StreamFactory {

    @Override
    public InputStreamReader getStreamFromFile(String fileName) {

        File file = new File(fileName);

        if(!file.exists()){
            throw new DocGenException("Failed to load fileName for resource: " + fileName);
        }

        InputStreamReader streamReader;

        try {
            streamReader = new FileReader(file);
        } catch (FileNotFoundException e) {
            throw new DocGenException("Failed to load fileName for resource: " + fileName);
        }

        return streamReader;
    }
}
