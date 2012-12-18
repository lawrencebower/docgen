package org.lawrencebower.docgen.core.generator.overlay;

public abstract class OverlayDocumentFactory {

    public OverlayDocument getOverlayDocument(String docName){
        OverlayDocument overlayDocument = getOverlayDocument();
        overlayDocument.setName(docName);
        return overlayDocument;
    }

    abstract OverlayDocument getOverlayDocument();

}
