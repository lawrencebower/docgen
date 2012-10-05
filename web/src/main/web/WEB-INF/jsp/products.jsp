<%@ include file="/WEB-INF/jsp/include.jsp" %>

hello I am the home page

<jsp:useBean id="products" scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_logic.view.product.ProductView>"/>
<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>

<div>
    <ol>
        <c:forEach var="product" items="${products}">

            <s:url var="product_url" value="/productSelect/productId/{productId}">
                <s:param name="productId" value="${product.id}"/>
            </s:url>

            <a href="${product_url}">
                <c:out value="${product.id}"/>
            </a>
        </c:forEach>
    </ol>
    <ol>
        <c:forEach var="selectedProduct" items="${sessionData.selectedProducts}">

            <c:out value="${selectedProduct.id}"/>
        </c:forEach>
    </ol>
</div>
