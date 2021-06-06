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
                <link href="{$baseUrl}/static/bootstrap.css" rel="stylesheet"/>
                <link
                    href="{$baseUrl}/static/monadoc.svg"
                    rel="icon"
                    type="image/svg+xml"/>
            </head>
            <body>
                <header class="mb-3">
                    <nav class="navbar navbar-light bg-light">
                        <div class="container">
                            <a class="navbar-brand" href="{$baseUrl}/">Monadoc</a>
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
        <nav class="container">
            <ol class="breadcrumb">
                <li class="breadcrumb-item active">
                    Home
                </li>
            </ol>
        </nav>
        <p>
            &#x1f516; Better Haskell documentation.
        </p>
        <h2>
            Recent Uploads
        </h2>
        <ul>
            <xsl:for-each select="packages/package">
                <xsl:variable name="pkg" select="normalize-space(.)"/>
                <li>
                    <a href="{$baseUrl}/package/{$pkg}">
                        <xsl:value-of select="$pkg"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="package">
        <xsl:variable name="pkg" select="normalize-space(name)"/>
        <nav class="container">
            <ol class="breadcrumb">
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}">Home</a>
                </li>
                <li class="breadcrumb-item active">
                    <xsl:value-of select="$pkg"/>
                </li>
            </ol>
        </nav>
        <h2>
            <xsl:value-of select="$pkg"/>
        </h2>
        <ul>
            <xsl:for-each select="versions/version">
                <xsl:variable name="ver" select="normalize-space(.)"/>
                <li>
                    <a href="{$baseUrl}/package/{$pkg}/{$ver}">
                        <xsl:value-of select="$ver"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="version">
        <xsl:variable name="pkg" select="normalize-space(name)"/>
        <xsl:variable name="ver" select="normalize-space(version)"/>
        <nav class="container">
            <ol class="breadcrumb">
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}">Home</a>
                </li>
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}/package/{$pkg}"><xsl:value-of select="$pkg"/></a>
                </li>
                <li class="breadcrumb-item active">
                    <xsl:value-of select="$ver"/>
                </li>
            </ol>
        </nav>
        <h2>
            <xsl:value-of select="$pkg"/>
            version
            <xsl:value-of select="$ver"/>
        </h2>
        <ul>
            <xsl:for-each select="revisions/revision">
                <xsl:variable name="rev" select="normalize-space(.)"/>
                <li>
                    <a href="{$baseUrl}/package/{$pkg}/{$ver}/{$rev}">
                        <xsl:value-of select="$rev"/>
                    </a>
                </li>
            </xsl:for-each>
        </ul>
    </xsl:template>

    <xsl:template match="revision">
        <xsl:variable name="pkg" select="normalize-space(name)"/>
        <xsl:variable name="ver" select="normalize-space(version)"/>
        <xsl:variable name="rev" select="normalize-space(revision)"/>
        <nav class="container">
            <ol class="breadcrumb">
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}">Home</a>
                </li>
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}/package/{$pkg}"><xsl:value-of select="$pkg"/></a>
                </li>
                <li class="breadcrumb-item">
                    <a href="{$baseUrl}/package/{$pkg}/{$ver}"><xsl:value-of select="$ver"/></a>
                </li>
                <li class="breadcrumb-item active">
                    <xsl:value-of select="$rev"/>
                </li>
            </ol>
        </nav>
        <h2>
            <xsl:value-of select="$pkg"/>
            version
            <xsl:value-of select="$ver"/>
            revision
            <xsl:value-of select="$rev"/>
        </h2>
    </xsl:template>
</xsl:stylesheet>
