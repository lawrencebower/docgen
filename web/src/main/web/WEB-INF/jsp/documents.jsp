<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>

<div>
    <ol>
        <c:forEach var="document" items="${sessionData.documents}">
            <s:url var="document_url" value="/documentSelect/document/{documentName}">
                <s:param name="documentName" value="${document.name}"/>
            </s:url>

            <a href="${document_url}">
                <c:out value="${document.name}"/><br>
            </a>
        </c:forEach>
    </ol>
</div>
