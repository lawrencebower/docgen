package org.lawrencebower.docgen.web_logic.view.document_info;

import java.util.*;

public class DocumentInfoSet {

    private List<DocumentInfoView> documents = new ArrayList<>();

    private DocumentInfoSet() {//force spring creation
    }

    public void setDocuments(Collection<DocumentInfoView> documents) {
        this.documents = new ArrayList<>(documents);
    }

    public void setDocuments(DocumentInfoView... views) {
        List<DocumentInfoView> docList = Arrays.asList(views);
        documents = new ArrayList<>(docList);
    }

    public List<DocumentInfoView> getDocumentsAsList() {
        return Collections.unmodifiableList(documents);
    }
}
