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
            byte[] bytes = new byte[(int) fileLength];

            BufferedReader reader = new BufferedReader(new FileReader(inFile));

            String currentLine;
            int offset = 0;
            int totalSize = 0;
            FileOutputStream fos = new FileOutputStream("C:\\code\\output\\testy.pdf");

            while ((currentLine = reader.readLine()) != null) {

                boolean includeLine = isLineIncluded(currentLine);

                if (includeLine) {

                    byte[] lineBytes = currentLine.getBytes("UTF-8");
                    fos.write(lineBytes);
                    fos.write("\n".getBytes());


                    int lineLength = lineBytes.length;
//                    totalSize += lineLength;
//                    System.out.println("totalSize = " + totalSize);

//                    System.out.println("offset = " + offset);
//                    System.out.println("lineLength = " + lineLength);

                    System.arraycopy(lineBytes,
                                     0,
                                     bytes,
                                     offset,
                                     lineLength);

                    offset += lineLength;
                }
            }

            fos.close();

            return bytes;

        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private boolean isLineIncluded(String currentLine) {

//        System.out.println("currentLine = " + currentLine);

        String trimmedLine = currentLine.trim();

//        for (String token : EXCLUDED_TOKENS) {
//            if (trimmedLine.contains(token)) {
//                return false;
//            }
//        }

        return true;
    }
}
