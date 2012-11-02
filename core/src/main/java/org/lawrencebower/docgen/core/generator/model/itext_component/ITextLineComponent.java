package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.pdf.draw.LineSeparator;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;

public class ITextLineComponent extends AbstractITextComponent<LineSeparator, LineComponent> {

    @Override
    public LineSeparator createITextComponent() {
        HorizontalAlignment alignment = component.getAlignment();
        int boxAlignment = HorizontalAlignment.mapToITextAlignment(alignment);

        return new LineSeparator(1,
                                 component.getWidthPercentage(),
                                 null,
                                 boxAlignment,
                                 0);
    }
}
