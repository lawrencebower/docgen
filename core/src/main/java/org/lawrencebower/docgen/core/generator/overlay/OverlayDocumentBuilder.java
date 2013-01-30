package org.lawrencebower.docgen.core.generator.overlay;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;

public class OverlayDocumentBuilder {

    @Autowired
    private OverlayDocumentFactory documentFactory;
    @Autowired
    private OverlayComponentFactory customComponentFactory;

    private OverlayDocument document;

    public OverlayDocument getDocument() {
        return document;
    }

    public void createDocument(String name, Resource overlayPath) {
        document = documentFactory.getOverlayDocument(name);
        document.setSourcePDF(overlayPath);
    }

    public void addComponent(DocComponent component) {
        OverlayComponent overlayComponent = convertComponent(component);
        document.addComponent(overlayComponent);
    }

    private OverlayComponent convertComponent(DocComponent component) {
        return customComponentFactory.createOverlayComponent(component);
    }

}
