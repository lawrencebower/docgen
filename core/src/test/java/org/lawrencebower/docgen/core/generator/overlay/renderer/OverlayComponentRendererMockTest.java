package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRenderer;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.custom.renderer.*;
import org.mockito.Mockito;

import java.io.ByteArrayOutputStream;

import static org.mockito.Mockito.verify;

public class OverlayComponentRendererMockTest {

    private CustomComponentRenderer renderer;

    private CustomTextRenderer mockTextRenderer = Mockito.mock(CustomTextRenderer.class);
    private CustomTableRenderer mockTableRenderer = Mockito.mock(CustomTableRenderer.class);
    private CustomNewLineRenderer mockNewLineRenderer = Mockito.mock(CustomNewLineRenderer.class);
    private CustomImageRenderer mockImageRenderer = Mockito.mock(CustomImageRenderer.class);
    private CustomTableTextRenderer mockTableTextRenderer = Mockito.mock(CustomTableTextRenderer.class);
    private CustomLineRenderer mockLineRenderer = Mockito.mock(CustomLineRenderer.class);

    @Before
    public void setup(){
        renderer = new CustomComponentRenderer();
        renderer.setImageRenderer(mockImageRenderer);
        renderer.setLineRenderer(mockLineRenderer);
        renderer.setNewLineRenderer(mockNewLineRenderer);
        renderer.setTableRenderer(mockTableRenderer);
        renderer.setTableTextRenderer(mockTableTextRenderer);
        renderer.setTextRenderer(mockTextRenderer);
    }

    @Test
    public void testCreateAndRenderComponent_passTextComponent_correctRendererCalled() throws Exception {

        TextComponent component = new TextComponent("hello");

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockTextRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateAndRenderComponent_passTableComponent_correctRendererCalled() throws Exception {

        TableComponent component = new TableComponent("table name");

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockTableRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateAndRenderComponent_passNewLineComponent_correctRendererCalled() throws Exception {

        NewLineComponent component = new NewLineComponent();

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockNewLineRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateAndRenderComponent_passImageComponent_correctRendererCalled() throws Exception {

        ImageComponent component = new ImageComponent("made up location");

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockImageRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateAndRenderComponent_passTableTextComponent_correctRendererCalled() throws Exception {

        TableTextComponent component = new TableTextComponent("text");

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockTableTextRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateAndRenderComponent_passLineComponent_correctRendererCalled() throws Exception {

        LineComponent component = new LineComponent(100);

        CustomComponentRendererInfo rendererInfo = makeRendererInfoAndRenderComponentWithMocks(component);

        verify(mockLineRenderer).createAndRenderComponent(component, rendererInfo);
    }

    @Test
    public void testCreateComponent_passTextComponent_correctRendererCalled() throws Exception {

        TextComponent component = new TextComponent("text");

        createComponentWithMocks(component);

        verify(mockTextRenderer).createComponent(component);
    }

    @Test
    public void testCreateComponent_passTableComponent_correctRendererCalled() throws Exception {

        TableComponent component = new TableComponent("name");

        createComponentWithMocks(component);

        verify(mockTableRenderer).createComponent(component);
    }

    @Test
    public void testCreateComponent_passNewLineComponent_correctRendererCalled() throws Exception {

        NewLineComponent component = new NewLineComponent();

        createComponentWithMocks(component);

        verify(mockNewLineRenderer).createComponent(component);
    }

    @Test
    public void testCreateComponent_passImageComponent_correctRendererCalled() throws Exception {

        ImageComponent component = new ImageComponent("made up location");

        createComponentWithMocks(component);

        verify(mockImageRenderer).createComponent(component);
    }

    @Test
    public void testCreateComponent_passTableTextComponent_correctRendererCalled() throws Exception {

        TableTextComponent component = new TableTextComponent("table");

        createComponentWithMocks(component);

        verify(mockTableTextRenderer).createComponent(component);
    }

    @Test
    public void testCreateComponent_passLineComponent_correctRendererCalled() throws Exception {

        LineComponent component = new LineComponent(100);

        createComponentWithMocks(component);

        verify(mockLineRenderer).createComponent(component);
    }

    private DocComponent getUnknownComponent() {
        return new DocComponent() {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };
    }

    private CustomComponentRendererInfo makeRendererInfoAndRenderComponentWithMocks(DocComponent component) {

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        renderer.createAndRenderComponent(component, rendererInfo);

        return rendererInfo;
    }

    private void createComponentWithMocks(DocComponent component) {

        renderer.createComponent(component);

    }

}
