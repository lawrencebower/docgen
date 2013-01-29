package org.lawrencebower.docgen.core.generator.utils;

import java.io.InputStream;

public interface StreamFactory {
    InputStream getStreamFromFile(String fileName);
}
