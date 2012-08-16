package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Chunk;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;

public class CustomNewLineRenderer implements DocComponentRenderer<NewLineComponent, CustomComponentRendererInfo> {

    @Override
    public void renderComponent(NewLineComponent component, CustomComponentRendererInfo rendererInfo) {
        drawTextBox(rendererInfo, component);
    }

    private void drawTextBox(CustomComponentRendererInfo renderInfo, NewLineComponent component) {
        renderInfo.addToDocument(Chunk.NEWLINE);
    }

}
