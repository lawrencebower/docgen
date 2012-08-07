package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.*;
import java.util.Arrays;
import java.util.List;

public class TestFileLineFilter {

    private static List<String> EXCLUDED_TOKENS;

    static{
        EXCLUDED_TOKENS = Arrays.asList("<</Producer",
                                        "<</Root");
    }

    public byte[] filterFileLines(File inFile) {

        try {

            long fileLength = inFile.length();
            System.out.println("fileLength = " + fileLength);
            ByteArrayOutputStream bytes = new ByteArrayOutputStream();

            BufferedReader reader = new BufferedReader(new FileReader(inFile));

            String currentLine;

            while ((currentLine = reader.readLine()) != null) {

                boolean includeLine = isLineIncluded(currentLine);

                if (includeLine) {

                    byte[] lineBytes = currentLine.getBytes();
                    bytes.write(lineBytes);
                }
            }

            return bytes.toByteArray();

        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private boolean isLineIncluded(String currentLine) {

        String trimmedLine = currentLine.trim();

        for (String token : EXCLUDED_TOKENS) {
            if (trimmedLine.contains(token)) {
                return false;
            }
        }

        return true;
    }
}
