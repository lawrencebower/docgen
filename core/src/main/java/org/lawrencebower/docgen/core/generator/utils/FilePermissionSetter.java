package org.lawrencebower.docgen.core.generator.utils;

import java.io.File;

public abstract class FilePermissionSetter {

    public abstract void setFilePermissions(File file);

    public static FilePermissionSetter getPermissionSetter() {
        /**
         * feel free to implement other os-specific permission setters...
         */
        return new LinuxFilePermissionSetter();
    }
}
