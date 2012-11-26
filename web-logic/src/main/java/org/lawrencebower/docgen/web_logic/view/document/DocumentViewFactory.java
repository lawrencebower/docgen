package org.lawrencebower.docgen.web_logic.view.document;

import org.lawrencebower.docgen.core.document.Document;

public abstract class DocumentViewFactory {

    public DocumentView createDocumentInfoView(Document doc){
        DocumentView documentViews = createDocumentInfoView();
        documentViews.setDocument(doc);
        return documentViews;
    }

    public abstract DocumentView createDocumentInfoView();

}
