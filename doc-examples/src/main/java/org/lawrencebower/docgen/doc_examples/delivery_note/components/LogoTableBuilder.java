package org.lawrencebower.docgen.doc_examples.delivery_note.components;

import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.springframework.core.io.Resource;

public class LogoTableBuilder {

    public TableComponent buildLogoTable(Resource imageResource) {

        ImageComponent logo = new ImageComponent(imageResource);
        logo.setSize(70, 1);

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("logo table");
        tableBuilder.makeEmptyHeaderRowWithColSpans(2);

        LayoutRow row = tableBuilder.makeRow();

        LayoutCell logoCell = new LayoutCell(logo);
        row.addCell(logoCell);

        TextBlock sloganBlock = new TextBlock("DELIVERY NOTE",
                                              new FontInfo(FontInfo.DEFAULT_FONT,
                                                           24,
                                                           FontStyle.BOLD));

        TableTextComponent sloganComponent = new TableTextComponent(sloganBlock);
        sloganComponent.setAlignment(HorizontalAlignment.RIGHT);
        LayoutCell sloganCell = new LayoutCell(sloganComponent);
        sloganCell.setVerticalAlignment(VerticalAlignment.BOTTOM);
        row.addCell(sloganCell);

        tableBuilder.addRow(row);

        tableBuilder.setWidthPercentage(100);

        return tableBuilder.getTable();
    }
}
