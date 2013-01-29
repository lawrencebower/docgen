package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class ChecksumUtils {

    @Autowired
    private TestFileLineFilter lineFilter;
    @Autowired
    private DocGenFileUtils fileUtils;

    public String getChecksumFromFile(File inputFile) {
        String result;
        FileInputStream inStream;

        try {
            inStream = new FileInputStream(inputFile);
            result = getChecksumFromInStream(inStream);
        } catch (Exception e) {
            throw new DocGenException(e);
        }
        return result;
    }

    public String getChecksumFromBytes(byte[] bytes) {
        String result;
        ByteArrayInputStream inStream;

        try {
            inStream = new ByteArrayInputStream(bytes);
            result = getChecksumFromInStream(inStream);
        } catch (Exception e) {
            throw new DocGenException(e);
        }
        return result;
    }

    private String getChecksumFromInStream(InputStream inStream) {
        String result;
        byte[] b = createChecksumBytes(inStream);
        result = "";
        for (int i = 0; i < b.length; i++) {
            result += Integer.toString((b[i] & 0xff) + 0x100, 16).substring(1);
        }
        return result;
    }

    private byte[] createChecksumBytes(InputStream is) {
        try {
            byte[] buffer = new byte[1024];
            MessageDigest complete = null;
            complete = MessageDigest.getInstance("MD5");
            int numRead;
            do {
                numRead = is.read(buffer);
                if (numRead > 0) {
                    complete.update(buffer, 0, numRead);
                }
            } while (numRead != -1);

            return complete.digest();

        } catch (NoSuchAlgorithmException | IOException e) {
            throw new DocGenException(e);
        }
    }

    public boolean fileChecksumsAreSame(File expectedFile, File outputFile) {

        String expectedChecksum = getChecksumFromFile(expectedFile);
        String outputChecksum = getChecksumFromFile(outputFile);

        return expectedChecksum.equals(outputChecksum);
    }

    public boolean filteredFileChecksumsAreSame(File expectedFile, File outputFile) {

        byte[] filteredExpectedFile = lineFilter.filterFileLines(expectedFile);
        String expectedChecksum = getChecksumFromBytes(filteredExpectedFile);
//        fileUtils.writeBytesToFile(filteredExpectedFile, new File("C:\\code\\expected_" + expectedFile.getName()));

        byte[] filteredOutputFile = lineFilter.filterFileLines(outputFile);
        String outputChecksum = getChecksumFromBytes(filteredOutputFile);
//        fileUtils.writeBytesToFile(filteredExpectedFile, new File("C:\\code\\actual_" + outputFile.getName()));

        return expectedChecksum.equals(outputChecksum);
    }
}
