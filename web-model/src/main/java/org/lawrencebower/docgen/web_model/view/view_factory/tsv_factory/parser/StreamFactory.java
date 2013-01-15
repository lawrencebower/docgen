package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import java.io.InputStreamReader;

public interface StreamFactory {
    InputStreamReader getStreamFromFile(String file);
}
