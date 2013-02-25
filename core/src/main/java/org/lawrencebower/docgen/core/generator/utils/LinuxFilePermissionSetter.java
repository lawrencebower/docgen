package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.IOException;

public class LinuxFilePermissionSetter extends FilePermissionSetter {

    @Override
    public void setFilePermissions(File file) {
        String os = System.getProperty("os.name");
        if("Linux".equals(os)){
            String filePath = file.getPath();
            try {
                Runtime runtime = Runtime.getRuntime();
                String command = "chmod 777 " + filePath;
                System.out.println("command = " + command);
                runtime.exec(command);
            } catch (IOException e) {
                String message = String.format("problem setting permissions on '%s'", filePath);
                throw new DocGenException(message);
            }
        }
    }
}
