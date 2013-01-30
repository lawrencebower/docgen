package org.lawrencebower.docgen.doc_examples.commercial_invoice.components;

import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.springframework.core.io.Resource;

public class SignatureTableBuilder {

    public TableComponent buildTable(Resource signatureResource) {

        ImageComponent sigImage = new ImageComponent(signatureResource);
        sigImage.setSize(15, 15);

        TextComponent printedSig = new TextComponent("brian");
        printedSig.setAlignment(HorizontalAlignment.RIGHT);

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("signature table");
        tableBuilder.makeEmptyHeaderRowWithColSpans(1, 1);

        LayoutCell sigImageCell = tableBuilder.makeCell(sigImage);
        LayoutCell printedSigCell = tableBuilder.makeCell(printedSig);
        printedSigCell.setVerticalAlignment(VerticalAlignment.BOTTOM);

        LayoutRow row = tableBuilder.makeRow();
        row.addCell(sigImageCell);
        row.addCell(printedSigCell);

        tableBuilder.addRow(row);

        tableBuilder.setWidthPercentage(25);

        return tableBuilder.getTable();
    }
}
