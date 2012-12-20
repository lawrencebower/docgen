package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomDocumentBuilder {

    @Autowired
    private CustomDocumentFactory documentFactory;
    @Autowired
    private CustomComponentFactory customComponentFactory;

    private CustomDocument document;

    public void createDocument(String name) {
        document = documentFactory.getCustomDocument(name);
    }

    public CustomDocument getDocument() {
        return document;
    }

    public void addComponent(DocComponent component) {
        CustomComponent customComponent = convertComponent(component);
        document.addComponent(customComponent);
    }

    private CustomComponent convertComponent(DocComponent component) {
        return customComponentFactory.createCustomComponent(component);
    }
}
