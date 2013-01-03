package org.lawrencebower.docgen.web_logic.view.view_factory.factory;

import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewImpl;

import java.util.List;

public interface DocumentFactory {
    List<DocumentView> getAllDocuments();

    DocumentViewImpl createDocument(String documentName);
}
