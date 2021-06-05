<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html"/>

    <xsl:variable name="baseUrl" select="normalize-space(/monadoc/config/baseUrl)"/>
    <xsl:variable name="clientId" select="normalize-space(/monadoc/config/clientId)"/>
    <xsl:variable name="user" select="normalize-space(/monadoc/config/user)"/>
    <xsl:variable name="version" select="normalize-space(/monadoc/config/version)"/>

    <xsl:template match="/monadoc">
        <html lang="en-US">
            <head>
                <meta content="initial-scale = 1, width = device-width" name="viewport"/>
                <title>
                    Monadoc
                </title>
                <link href="{$baseUrl}/bootstrap.css" rel="stylesheet"/>
                <link
                    href="{$baseUrl}/monadoc.svg"
                    rel="icon"
                    type="image/svg+xml"/>
            </head>
            <body>
                <header class="mb-3">
                    <nav class="navbar navbar-light bg-light">
                        <div class="container">
                            <h1 class="fs-6">
                                <a class="navbar-brand" href="{$baseUrl}/">Monadoc</a>
                            </h1>
                            <ul class="navbar-nav">
                                <li class="nav-item">
                                    <xsl:choose>
                                        <xsl:when test="$user">
                                            <a class="nav-link" href="#TODO">@<xsl:value-of select="$user"/></a>
                                        </xsl:when>
                                        <xsl:otherwise>
                                            <a class="nav-link" href="https://github.com/login/oauth/authorize?client_id={$clientId}&amp;redirect_uri={$baseUrl}/oauth/callback">Log in</a>
                                        </xsl:otherwise>
                                    </xsl:choose>
                                </li>
                            </ul>
                        </div>
                    </nav>
                </header>
                <main class="container mt-3 mb-3">
                    <xsl:apply-templates select="page"/>
                </main>
                <footer class="container pt-3 mt-3 text-muted border-top">
                    <p>
                        Powered by
                        <a href="https://github.com/tfausak/monadoc">Monadoc</a>
                        version
                        <xsl:value-of select="$version"/>.
                    </p>
                </footer>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="index">
        <p>
            &#x1f516; Better Haskell documentation.
        </p>
    </xsl:template>

    <xsl:template match="package">
        <p>
            TODO <xsl:value-of select="name"/>
        </p>
    </xsl:template>
</xsl:stylesheet>
