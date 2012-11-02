package org.lawrencebower.docgen.core.document;

import org.lawrencebower.docgen.core.document.type.DocType;

import java.util.List;

public interface DocumentInfo<T extends RenderableComponent> {

    String getName();

    void setComponents(List<T> components);

    List<T> getComponents();

    DocType getDocType();

    PDFDocument generatePDF();

    void setName(String name);

    void addComponent(T component);
}
