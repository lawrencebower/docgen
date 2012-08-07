package org.lawrencebower.docgen.core.generator.utils;

import java.io.File;

public class DocGenFileUtils {

    public void deleteFileIfAlreadyExists(File file) {
        org.apache.commons.io.FileUtils.deleteQuietly(file);
    }
}
