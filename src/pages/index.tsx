import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import styles from './index.module.css';

// Feature data structure
const FeatureList = [
  {
    title: 'AI-Powered Modernization',
    icon: '🤖',
    description: 'Leverage cutting-edge AI tools like Cursor and ChatGPT Enterprise to accelerate COBOL modernization with intelligent code suggestions and automated refactoring.',
  },
  {
    title: 'Proven Workflows',
    icon: '⚡',
    description: 'Follow battle-tested workflows and best practices developed through real-world modernization projects at Global Shop Solutions.',
  },
  {
    title: 'Comprehensive Templates',
    icon: '📋',
    description: 'Access ready-to-use prompt templates, code patterns, and documentation templates to jumpstart your modernization efforts.',
  },
  {
    title: 'Multi-Tool Integration',
    icon: '🔧',
    description: 'Integrate seamlessly with Cursor, ChatGPT Enterprise, and OpenAI Codex to create a powerful AI-assisted development environment.',
  },
  {
    title: 'Best Practices',
    icon: '✅',
    description: 'Learn industry best practices for code review, testing, security, and quality assurance in AI-assisted development.',
  },
  {
    title: 'Troubleshooting Guide',
    icon: '🔍',
    description: 'Quickly resolve common issues with our comprehensive troubleshooting guide and solutions database.',
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

const Stats = [
  { value: '10x', label: 'Faster Development' },
  { value: '85%', label: 'Code Accuracy' },
  { value: '50+', label: 'Ready Templates' },
  { value: '24/7', label: 'AI Assistance' },
];

function Feature({ title, icon, description }) {
  return (
    <div className={clsx('col col--4', styles.feature)}>
      <div className={styles.featureCard}>
        <div className={styles.featureIcon}>{icon}</div>
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
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
              Get Started →
            </Link>
            <Link
              className={clsx('button button--outline button--lg', styles.secondaryButton)}
              to="/docs/workflow"
            >
              View Workflow
            </Link>
          </div>
        </div>
        <div className={styles.statsContainer}>
          {Stats.map((stat, idx) => (
            <div key={idx} className={styles.statItem}>
              <div className={styles.statValue}>{stat.value}</div>
              <div className={styles.statLabel}>{stat.label}</div>
            </div>
          ))}
        </div>
      </div>
    </header>
  );
}

function FeaturesSection() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className={styles.sectionHeader}>
          <h2>Why Choose Our AI Modernization Guide?</h2>
          <p>Everything you need to modernize legacy code with AI assistance</p>
        </div>
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
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
          <h2>Powerful AI Tools at Your Fingertips</h2>
          <p>Integrate the best AI development tools for maximum productivity</p>
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

function QuickStartSection() {
  return (
    <section className={styles.quickStart}>
      <div className="container">
        <div className={styles.sectionHeader}>
          <h2>Quick Start</h2>
          <p>Get up and running in minutes</p>
        </div>
        <div className={styles.quickStartGrid}>
          <div className={styles.quickStartCard}>
            <div className={styles.stepNumber}>1</div>
            <h3>Read the Overview</h3>
            <p>Understand the fundamentals of AI-assisted modernization</p>
            <Link to="/docs/overview" className={styles.cardLink}>
              Start Reading →
            </Link>
          </div>
          <div className={styles.quickStartCard}>
            <div className={styles.stepNumber}>2</div>
            <h3>Set Up Your Tools</h3>
            <p>Configure Cursor, ChatGPT, or Codex for your environment</p>
            <Link to="/docs/cursor" className={styles.cardLink}>
              Configure Tools →
            </Link>
          </div>
          <div className={styles.quickStartCard}>
            <div className={styles.stepNumber}>3</div>
            <h3>Follow the Workflow</h3>
            <p>Apply proven workflows to your modernization projects</p>
            <Link to="/docs/workflow" className={styles.cardLink}>
              View Workflow →
            </Link>
          </div>
        </div>
      </div>
    </section>
  );
}

function CTASection() {
  return (
    <section className={styles.ctaSection}>
      <div className="container">
        <div className={styles.ctaContent}>
          <h2>Ready to Modernize Your Legacy Code?</h2>
          <p>Start leveraging AI to accelerate your development today</p>
          <div className={styles.ctaButtons}>
            <Link
              className={clsx('button button--primary button--lg', styles.ctaButton)}
              to="/docs/overview"
            >
              Get Started Now
            </Link>
            <Link
              className={clsx('button button--outline button--lg', styles.ctaButtonSecondary)}
              to="/docs/prompt-library"
            >
              Explore Templates
            </Link>
          </div>
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
        <FeaturesSection />
        <ToolsSection />
        <QuickStartSection />
        <CTASection />
      </main>
    </Layout>
  );
}
