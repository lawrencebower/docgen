package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/** A file with rows with tab (\t) separated values. Comment lines
 * starting with a hash (#) and empty lines will be ignored.
 */
public class TSVReader {

    @Autowired(required = false)
    private DocGenFileUtils fileUtils;

	public static final String separator = "\t";
	public static final String comment = "#";
    public static final String EMPTY_COL = "(null)";

    public DataSet readDataSetAsFile(Resource fileName) {

        InputStream inputStream = fileUtils.getInputStreamFromResource(fileName);

        return readTSV(inputStream);
    }

    protected DataSet readTSV(InputStream inputStream) {

        InputStreamReader streamReader = new InputStreamReader(inputStream);

        BufferedReader reader = new BufferedReader(streamReader);

        DataSet result = new DataSet();

        while (true) {

            String line = readLine(reader);
            if (line == null) {
                break;
            }

            String trimmedLine = line.trim();

            if (trimmedLine.isEmpty()) {
                continue; // ignore empty lines
            }

            trimmedLine = removeLeadingAndTrailingQuotes(trimmedLine);

            if (trimmedLine.startsWith(comment)) {
                continue; // ignore comment lines
            }

            String[] tokens = StringUtils.splitPreserveAllTokens(line, separator);

            processTokens(tokens);

            result.addRow(new DataRow(tokens));
        }

        return result;
    }

    private String readLine(BufferedReader reader) {
        try {
            return reader.readLine();
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private void processTokens(String[] tokens) {
        for(int i = 0; i<tokens.length;i++){
            String processedToken = replaceNewlineChars(tokens[i]);
            processedToken = removeLeadingAndTrailingQuotes(processedToken);
            tokens[i] = processedToken;
        }
    }

    private String replaceNewlineChars(String token){
        return token.replace("\\n","\n");
    }

    private String removeLeadingAndTrailingQuotes(String string) {

        if(string.startsWith("\"")){
            string = string.replaceAll("^\"", "");
        }

        if(string.endsWith("\"")){
            string = string.replaceAll("\"$", "");
        }

        return string;
    }
}
