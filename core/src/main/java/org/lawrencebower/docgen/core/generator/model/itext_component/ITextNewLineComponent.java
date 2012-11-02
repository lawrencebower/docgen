package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Paragraph;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;

public class ITextNewLineComponent extends AbstractITextComponent<Paragraph, NewLineComponent> {

    @Override
    public Paragraph createITextComponent() {
        return new Paragraph("\n");
    }
}
