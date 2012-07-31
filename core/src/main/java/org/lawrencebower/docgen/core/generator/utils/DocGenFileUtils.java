package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.MessageDigest;

public class DocGenFileUtils {

    public void deleteFileIfAlreadyExists(File file) {
        org.apache.commons.io.FileUtils.deleteQuietly(file);
    }

    public String getChecksum(File inputFile) {
        String result;
        FileInputStream inStream = null;

        try {
            inStream = new FileInputStream(inputFile);
            byte[] b = createChecksum(inStream);
            result = "";
            for (int i = 0; i < b.length; i++) {
                result += Integer.toString((b[i] & 0xff) + 0x100, 16).substring(1);
            }
        } catch (Exception e) {
            throw new DocGenException(e);
        }
        return result;
    }

    private byte[] createChecksum(InputStream is) throws Exception {
        byte[] buffer = new byte[1024];
        MessageDigest complete = MessageDigest.getInstance("MD5");
        int numRead;
        do {
            numRead = is.read(buffer);
            if (numRead > 0) {
                complete.update(buffer, 0, numRead);
            }
        } while (numRead != -1);
        return complete.digest();
    }
}
