package org.lawrencebower.docgen.web_logic.view.document;

import java.util.Collection;

public abstract class DocumentSetFactory {

    public DocumentSet createDocumentInfoSet(Collection<DocumentView> documentViews){
        DocumentSet documentSet = createDocumentInfoSet();
        documentSet.setDocuments(documentViews);
        return documentSet;
    }

    public DocumentSet createDocumentInfoSet(DocumentView... documentViews){
        DocumentSet documentSet = createDocumentInfoSet();
        documentSet.setDocuments(documentViews);
        return documentSet;
    }

    public abstract DocumentSet createDocumentInfoSet();

}
