package org.lawrencebower.docgen.core.generator.utils;

import org.apache.commons.io.FileUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.IOException;

public class DocGenFileUtils {

    public void deleteQuietly(File file) {
        FileUtils.deleteQuietly(file);
    }

    public byte[] readFileToByteArray(File file) {
        try {
            return FileUtils.readFileToByteArray(file);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }
}
