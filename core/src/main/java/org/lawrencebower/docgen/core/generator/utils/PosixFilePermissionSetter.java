package org.lawrencebower.docgen.core.generator.utils;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;

import static java.nio.file.Files.setPosixFilePermissions;

public class PosixFilePermissionSetter extends FilePermissionSetter {

    static Logger logger = Logger.getLogger(PosixFilePermissionSetter.class);

    @Override
    public void setFilePermissions(File file) {
        try {
            Set<PosixFilePermission> permissions = getPermissions();
            Path nioPath = getPath(file);
            setPosixFilePermissions(nioPath, permissions);
        } catch (UnsupportedOperationException uo) {
            logger.error("POSIX file permissions not supported by this OS");
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private Path getPath(File file) {
        String path = file.getPath();
        return Paths.get(path);
    }

    private Set<PosixFilePermission> getPermissions() {
        Set<PosixFilePermission> permissions = new HashSet<>();
        permissions.add(PosixFilePermission.OTHERS_READ);
        permissions.add(PosixFilePermission.OTHERS_WRITE);
        return permissions;
    }
}
