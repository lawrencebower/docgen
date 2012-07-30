package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomComponentRenderer implements DocComponentRenderer<DocComponent, CustomComponentRendererInfo> {

    @Autowired
    private CustomTextRenderer textRenderer;

    @Override
    public void renderComponent(DocComponent component, CustomComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.renderComponent((TextComponent) component, rendererInfo);
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }

}
