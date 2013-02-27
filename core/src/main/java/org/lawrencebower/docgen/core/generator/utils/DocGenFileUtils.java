package org.lawrencebower.docgen.core.generator.utils;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;

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

    public void makeDirIfNotExists(File file) {
        if (!file.exists()) {
            boolean success = file.mkdirs();
            if (!success) {
                String filePath = file.getPath();
                String message = String.format("Could not make dirs for '%s'", filePath);
                throw new DocGenException(message);
            }
        }
    }

    public void writeBytesToFile(byte[] bytes, File file) {
        try {
            FileUtils.writeByteArrayToFile(file, bytes);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    public byte[] getAllBytesFromStream(InputStream inputStream) {
        try {
            return IOUtils.toByteArray(inputStream);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    public InputStream getInputStreamFromResource(Resource resource) {
        try {
            return resource.getInputStream();
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    public String createUUID() {
        UUID uuid = UUID.randomUUID();
        return uuid.toString();
    }

    public File classPathResourceToFile(String path) {
        try {
            Resource resource = new ClassPathResource(path);
            return resource.getFile();
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }
}
