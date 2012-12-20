package org.lawrencebower.docgen.core.generator.overlay;

public abstract class OverlayDocumentFactory {

    public OverlayDocument getOverlayDocument(String docName){
        OverlayDocument overlayDocument = getOverlayDocumentBean();
        overlayDocument.setName(docName);
        return overlayDocument;
    }

    protected abstract OverlayDocument getOverlayDocumentBean();
}
