package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayComponentRenderer implements DocComponentRenderer<DocComponent, OverlayComponentRendererInfo> {

    @Autowired
    private OverlayTextRenderer textRenderer;
    @Autowired
    private OverlayTableRenderer tableRenderer;
    @Autowired
    private OverlayCheckBoxRenderer checkBoxRenderer;

    @Override
    public void createAndRenderComponent(DocComponent component, OverlayComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.createAndRenderComponent((TextComponent) component, rendererInfo);
        }else if (component instanceof TableComponent) {
            tableRenderer.createAndRenderComponent((TableComponent) component, rendererInfo);
        }else if (component instanceof CheckBoxComponent) {
            checkBoxRenderer.createAndRenderComponent((CheckBoxComponent) component, rendererInfo);
        }else if (component instanceof NewLineComponent) {
            throw new UnsupportedOperationException("NewLine not supported by Overlay renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }
}
