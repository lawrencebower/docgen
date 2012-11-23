package org.lawrencebower.docgen.web_logic.view.document_info;

import java.util.Collection;

public abstract class DocumentInfoSetFactory {

    public DocumentInfoSet createDocumentInfoSet(Collection<DocumentInfoView> docInfoViews){
        DocumentInfoSet infoSet = createDocumentInfoSet();
        infoSet.setDocuments(docInfoViews);
        return infoSet;
    }

    public DocumentInfoSet createDocumentInfoSet(DocumentInfoView... docInfoViews){
        DocumentInfoSet infoSet = createDocumentInfoSet();
        infoSet.setDocuments(docInfoViews);
        return infoSet;
    }

    public abstract DocumentInfoSet createDocumentInfoSet();

}
