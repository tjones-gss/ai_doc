import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import styles from './index.module.css';

// Quick links for easy navigation
const QuickLinks = [
  {
    title: 'Getting Started',
    icon: '📖',
    description: 'Learn the basics of using AI tools for COBOL development.',
    link: '/docs/overview',
  },
  {
    title: 'Workflows',
    icon: '⚡',
    description: 'Step-by-step guides for common modernization tasks.',
    link: '/docs/workflow',
  },
  {
    title: 'Prompt Templates',
    icon: '📋',
    description: 'Ready-to-use prompts for various development scenarios.',
    link: '/docs/prompt-library',
  },
  {
    title: 'Troubleshooting',
    icon: '🔧',
    description: 'Solutions to common issues and problems.',
    link: '/docs/troubleshooting',
  },
];

const ToolsList = [
  {
    name: 'Cursor',
    description: 'AI-powered code editor for intelligent modernization',
    link: '/docs/cursor',
    icon: '💻',
  },
  {
    name: 'ChatGPT Enterprise',
    description: 'Conversational AI for complex refactoring tasks',
    link: '/docs/chatgpt',
    icon: '💬',
  },
  {
    name: 'OpenAI Codex',
    description: 'Advanced code generation and translation',
    link: '/docs/codex',
    icon: '🧠',
  },
];

function QuickLink({ title, icon, description, link }) {
  return (
    <div className={clsx('col col--6 col--lg-3', styles.quickLinkItem)}>
      <Link to={link} className={styles.quickLinkCard}>
        <div className={styles.quickLinkIcon}>{icon}</div>
        <h3>{title}</h3>
        <p>{description}</p>
      </Link>
    </div>
  );
}

function Tool({ name, description, link, icon }) {
  return (
    <div className={clsx('col col--4', styles.tool)}>
      <Link to={link} className={styles.toolCard}>
        <div className={styles.toolIcon}>{icon}</div>
        <h3>{name}</h3>
        <p>{description}</p>
        <span className={styles.toolLink}>Learn more →</span>
      </Link>
    </div>
  );
}

function HomepageHeader() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className={styles.heroBackground}></div>
      <div className="container">
        <div className={styles.heroContent}>
          <h1 className={styles.heroTitle}>{siteConfig.title}</h1>
          <p className={styles.heroSubtitle}>{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={clsx('button button--primary button--lg', styles.primaryButton)}
              to="/docs/overview"
            >
              Get Started
            </Link>
          </div>
        </div>
      </div>
    </header>
  );
}

function QuickLinksSection() {
  return (
    <section className={styles.quickLinks}>
      <div className="container">
        <div className={styles.sectionHeader}>
          <h2>Quick Links</h2>
          <p>Jump to the section you need</p>
        </div>
        <div className="row">
          {QuickLinks.map((props, idx) => (
            <QuickLink key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

function ToolsSection() {
  return (
    <section className={styles.toolsSection}>
      <div className="container">
        <div className={styles.sectionHeader}>
          <h2>Available AI Tools</h2>
          <p>Documentation for the AI tools we use</p>
        </div>
        <div className="row">
          {ToolsList.map((props, idx) => (
            <Tool key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}

export default function Home() {
  return (
    <Layout
      title={`Home`}
      description="AI-Assisted COBOL Modernization Guide for Global Shop Solutions"
    >
      <HomepageHeader />
      <main>
        <QuickLinksSection />
        <ToolsSection />
      </main>
    </Layout>
  );
}
