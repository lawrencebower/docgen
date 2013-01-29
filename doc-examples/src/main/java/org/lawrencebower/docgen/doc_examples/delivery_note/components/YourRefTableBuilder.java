package org.lawrencebower.docgen.doc_examples.delivery_note.components;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

public class YourRefTableBuilder {
    private DocumentViewBuilder documentViewBuilder;

    public YourRefTableBuilder(DocumentViewBuilder documentViewBuilder) {
        this.documentViewBuilder = documentViewBuilder;
    }

    public DocComponent buildYourRefTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("your ref table");

        tableBuilder.makeEmptyHeaderRowWithColSpans(2);

        TableTextComponent yourRefComponent = new TableTextComponent("your ref -");
        yourRefComponent.setAlignment(HorizontalAlignment.RIGHT);

        FontInfo fontInfo = FontInfo.DEFAULT_BOLD();
        TextBlock refNoBlock = new TextBlock("PO 42464", fontInfo);
        TableTextComponent refNumberComponent = new TableTextComponent(refNoBlock);
        refNumberComponent.setName("refNumber");
        refNumberComponent.setAlignment(HorizontalAlignment.RIGHT);

        documentViewBuilder.addViewableComponent(refNumberComponent);

        tableBuilder.addRowWithComponents(yourRefComponent, refNumberComponent);

        tableBuilder.setAlignment(HorizontalAlignment.RIGHT);

        tableBuilder.setWidthPercentage(20);

        tableBuilder.setTablePadding(0);

        return tableBuilder.getTable();
    }
}
