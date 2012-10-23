package org.lawrencebower.docgen.core.document;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;

import java.util.List;

public interface DocumentInfo {
    String getName();

    void setComponents(List<DocComponent> components);

    List<DocComponent> getComponents();

    DocType getDocType();

    PDFDocument generatePDF();

    void setName(String name);

    void addComponent(DocComponent component);
}
