package org.lawrencebower.docgen.web_logic.view.document;

import org.lawrencebower.docgen.core.document.Document;

public abstract class DocumentViewFactory {

    public DocumentView createDocumentView(Document doc){
        DocumentView documentView = createDocumentView();
        documentView.setDocument(doc);
        return documentView;
    }

    public abstract DocumentView createDocumentView();

}
